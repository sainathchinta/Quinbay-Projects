package com.gdn.x.product.service.impl;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.MasterDataUtil;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;

@Service
@Slf4j
public class MasterDataConstructorServiceImpl implements MasterDataConstructorService {

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private MasterDataCacheService masterDataCacheService;

  @Autowired
  @Lazy
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private ExecutorService executorService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${exclude.hide.on.customer.side.attributes.enabled}")
  private boolean excludeHideOnCustomerSideAttributesEnabled;

  @Value("${imei.attribute.code}")
  private String imeiAttributeCode;

  @Value("#{'${imei.attribute.allowed.values}'.split(',')}")
  private List<String> imeiAllowedValues;

  @Override
  public MasterDataItem constructItemDimensionFields(MasterDataItem masterDataItem,
      MasterDataProduct masterDataProduct) {
    return MasterDataUtil.constructItemDimensionFields(masterDataItem, masterDataProduct);
  }

  @Override
  public ProductAndItemsVO constructItemsDimensionFields(ProductAndItemsVO productAndItems) {
    for (Item item : productAndItems.getItems()) {
      if (item.getMasterDataItem() != null) {
        item.setMasterDataItem(this.constructItemDimensionFields(item.getMasterDataItem(),
            productAndItems.getProduct().getMasterDataProduct()));
      }
    }
    return productAndItems;
  }

  @Override
  public ProductAndItemsVO constructProductAndItemWithMasterData(String storeId, String username,
      String requestId, Product product, List<Item> items) throws Exception {
    MasterDataProductAndItemsVO masterDataProductAndItems = null;
    if (StringUtils.isNotBlank(product.getProductCode())) {
      Set<String> productCodes = new HashSet<String>();
      productCodes.add(product.getProductCode());
        masterDataProductAndItems = this.masterDataService
            .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false).get
                (product.getProductCode());
    }
    return constructProductWithMasterDataAndSpecialField(product, items, masterDataProductAndItems, isProductVisibilityEnabled);
  }

  @Override
  public ProductAndItemsVO constructProductAndItemWithMasterData(String storeId, String username,
      String requestId, Product product, List<Item> items, boolean isPVSwitchEnabled, boolean inAllProducts) throws Exception {
    MasterDataProductAndItemsVO masterDataProductAndItems = null;
    Set<String> productCodes = new HashSet<String>();
    if (StringUtils.isNotBlank(product.getProductCode())) {
      productCodes.add(product.getProductCode());
      if (isPVSwitchEnabled) {
        String defaultProductCode = StringUtils.EMPTY;
        for (Item item : items) {
          if (item.getPristineDataItem() != null && StringUtils
              .isNotEmpty(item.getPristineDataItem().getDefaultProductCode())) {
            defaultProductCode = item.getPristineDataItem().getDefaultProductCode();
            productCodes.add(defaultProductCode);
            break;
          }
        }
        Map<String, MasterDataProductAndItemsVO> response = this.masterDataService
            .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, inAllProducts);
        Optional<MasterDataProductAndItemsVO> defaultProductOptional =
            Optional.ofNullable(response.get(defaultProductCode));
        Optional<MasterDataProductAndItemsVO> productOptional=
            Optional.ofNullable(response.get(product.getProductCode()));
        if (defaultProductOptional.isPresent() && productOptional.isPresent() && !defaultProductCode
            .equals(product.getProductCode())) {
          CommonUtil.updateMasterDataWithDPCMasterData(response.get(product.getProductCode())
              .getMasterDataProduct(), response.get(defaultProductCode).getMasterDataProduct());
        }
        masterDataProductAndItems = productOptional.orElse(null);
      } else {
        masterDataProductAndItems = this.masterDataService
            .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, inAllProducts).get
                (product.getProductCode());
      }
    }
    if (excludeHideOnCustomerSideAttributesEnabled) {
      setImeiRequiredField(product);
      removeHideCustomerAttributeFromResponse(product, masterDataProductAndItems);
    }
    return constructProductWithMasterDataAndSpecialField(product, items, masterDataProductAndItems, isPVSwitchEnabled);
  }


  private static void removeHideCustomerAttributeFromResponse(Product product,
      MasterDataProductAndItemsVO masterDataProductAndItems) {
    if (Objects.nonNull(masterDataProductAndItems)) {
      List<ProductSpecialAttribute> originalSpecialAttributes =
          Optional.ofNullable(product.getProductSpecialAttributes()).orElse(new ArrayList<>());
      List<MasterDataProductAttribute> originalMasterDataAttributes =
          Optional.ofNullable(masterDataProductAndItems.getMasterDataProduct()).orElse(new MasterDataProduct())
              .getMasterDataProductAttributes();
      try {
        List<MasterDataProductAttribute> masterDataAttributes =
            Optional.ofNullable(masterDataProductAndItems.getMasterDataProduct()).orElse(new MasterDataProduct())
                .getMasterDataProductAttributes();
        if (CollectionUtils.isNotEmpty(masterDataAttributes)) {
          masterDataAttributes.removeIf(
              attribute -> Objects.nonNull(attribute) && Objects.nonNull(attribute.getMasterDataAttribute())
                  && attribute.getMasterDataAttribute().isHideOnCustomerSide());
        }
        List<ProductSpecialAttribute> customerShowingSpecialAttributes =
            originalSpecialAttributes.stream().filter(Objects::nonNull).filter(
                productSpecialAttribute -> masterDataAttributes.stream().filter(Objects::nonNull).anyMatch(
                    masterDataProductAttribute -> Objects.nonNull(masterDataProductAttribute.getMasterDataAttribute())
                        && Objects.equals(masterDataProductAttribute.getMasterDataAttribute().getAttributeCode(),
                        productSpecialAttribute.getAttributeCode())
                        && !masterDataProductAttribute.getMasterDataAttribute().isHideOnCustomerSide())).toList();
        product.setProductSpecialAttributes(customerShowingSpecialAttributes);
      } catch (Exception e) {
        log.error("Error during removal of hide from customer side attributes for product code {} : ",
            product.getProductCode(), e);
        masterDataProductAndItems.getMasterDataProduct().setMasterDataProductAttributes(originalMasterDataAttributes);
        product.setProductSpecialAttributes(originalSpecialAttributes);
      }
    }
  }

  private void setImeiRequiredField(Product product) {
    if (StringUtils.isNotBlank(imeiAttributeCode)) {
      Optional.ofNullable(product.getProductSpecialAttributes()).filter(CollectionUtils::isNotEmpty).map(
          specialAttributes -> specialAttributes.stream().filter(Objects::nonNull)
              .filter(specialAttribute -> StringUtils.equals(specialAttribute.getAttributeCode(), imeiAttributeCode))
              .collect(Collectors.toMap(ProductSpecialAttribute::getAttributeCode,
                  ProductSpecialAttribute::getAttributeValue, (old, newValue) -> newValue))).ifPresent(attributeMap -> {
        String imeiValue = attributeMap.get(imeiAttributeCode);
        product.setImeiRequired(imeiAllowedValues.contains(imeiValue));
      });
    }
  }


  @Override
  public List<ProductAndItemsVO> constructProductsAndItemsWithMasterData(String storeId,
      String username, String requestId, Map<String, Product> productMap, List<Item> items,
      boolean inAllProducts)
      throws Exception {
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    Map<String, MasterDataProductAndItemsVO> masterDataDetailResponse = new HashMap<>();

    Set<String> productCodes =
        productMap.values().stream().filter(e -> StringUtils.isNotBlank(e.getProductCode()))
            .map(Product::getProductCode).collect(toSet());
    if (!productCodes.isEmpty()) {
      masterDataDetailResponse = masterDataService.getMasterDataProductDetailResponse(storeId,
          username, requestId, productCodes, inAllProducts);
    }
    for (Item item : items) {
      Product product = productMap.get(item.getProductSku());
      productAndItemsVOs.add(
          constructProductWithMasterDataAndSpecialField(product, Stream.of(item).collect(toList()),
              masterDataDetailResponse.get(product.getProductCode()), isProductVisibilityEnabled));
    }
    return productAndItemsVOs;
  }

  private ProductAndItemsVO constructProductWithMasterDataAndSpecialField(Product product,
      List<Item> items, MasterDataProductAndItemsVO masterDataProductAndItems, boolean isPVSwitch) {
    if (masterDataProductAndItems != null) {
      MasterDataProduct masterDataProduct = masterDataProductAndItems.getMasterDataProduct();
      if (valueTypeAdditionForDefiningAttributes) {
        product.setSizeAttributeCode(masterDataProduct.getSizeAttributeCode());
      }
      Map<String, MasterDataItem> masterDataItems = masterDataProductAndItems.getMasterDataItems();
      if (product.isSynchronized()) {
        product.setMasterDataProduct(masterDataProduct);
        for (Item item : items) {
          item.setMasterDataItem(masterDataItems.get(item.getItemCode()));
        }
      } else {
        // special field a.k.a field that always follow master data regardless sync status goes here
        product.getMasterDataProduct().setMasterCatalog(masterDataProduct.getMasterCatalog());
        product.getMasterDataProduct().setBrandLogoUrl(masterDataProduct.getBrandLogoUrl());
        product.getMasterDataProduct().setMasterDataProductImages(masterDataProduct.getMasterDataProductImages());
        }
        if(!CollectionUtils.isEmpty(items)){
          boolean hasPristineInfo = false;
          for (Item item : items) {
            MasterDataItem masterDataItem = masterDataItems.get(item.getItemCode());
            hasPristineInfo = hasPristineInfo || (item.getPristineDataItem() != null);
            if (Objects.nonNull(masterDataItem) && Objects.nonNull(item.getMasterDataItem())) {
              item.getMasterDataItem().setDangerousLevel(masterDataItem.getDangerousLevel());
              item.getMasterDataItem().setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
            }
          }
          if(isPVSwitch && hasPristineInfo){
            product.setMasterDataProduct(masterDataProduct);
          }
        }
      }
    return new ProductAndItemsVO(product, items);
  }

  @Override
  public ProductAndItemsVO constructProductAndItemWithMasterDataAndEvictCaches(String storeId,
      String username, String requestId, Product product, List<Item> items) throws Exception {
    List<String> itemCodes = new ArrayList<>();
    for(Item item : items){
      itemCodes.add(item.getItemCode());
    }
    this.masterDataCacheService.evictCachesMasterDataItem(itemCodes);
    List<String> productCodes = new ArrayList<>();
    productCodes.add(product.getProductCode());
    this.masterDataCacheService.evictCachesMasterDataProduct(productCodes);
    itemCodes.forEach(itemCode ->  this.masterDataCacheService.evictCacheMasterDataForTransaction(itemCode));
    
    ProductAndItemsVO productAndItemsVO = this.constructProductAndItemWithMasterData(storeId, username, requestId, product, items);
    List<ProductAttribute> definingAttributes = new ArrayList<>();
    for(Item item : productAndItemsVO.getItems()){
      ProductAttribute productAttribute = new ProductAttribute();
      productAttribute.setItemSku(item.getItemSku());

      List<ProductAttributeDetail> productAttributeDetails = new ArrayList<ProductAttributeDetail>();
      for (MasterDataItemAttributeValue itemAttributeValue : item.getMasterDataItem().getMasterDataItemAttributeValues()) {
        if (isDefiningOrVariantCreationTrue(itemAttributeValue)) {
          ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
          productAttributeDetail.setAttributeCode(itemAttributeValue.getMasterDataAttribute().getAttributeCode());
          productAttributeDetail.setAttributeName(itemAttributeValue.getMasterDataAttribute().getAttributeName());
          productAttributeDetail.setAttributeValue(itemAttributeValue.getAttributeValue());
          productAttributeDetails.add(productAttributeDetail);
        }
      }
      productAttribute.setProductAttributeDetails(productAttributeDetails);
      definingAttributes.add(productAttribute);
    }
    productAndItemsVO.getProduct().setDefiningAttributes(definingAttributes);
    
    return productAndItemsVO;
  }

  private boolean isDefiningOrVariantCreationTrue(MasterDataItemAttributeValue itemAttributeValue) {
    return MasterDataAttributeType.DEFINING_ATTRIBUTE
        .equals(itemAttributeValue.getMasterDataAttribute().getAttributeType()) || itemAttributeValue
        .getMasterDataAttribute().isVariantCreation();
  }


  @Override
  public Item constructPristineDataItemWithMasterData(Item item) throws Exception {
    Map<String, MasterDataItem> map;
    MasterDataItem masterDataItem = null;
    if (!item.isSynchronized()) {
      masterDataItem = item.getMasterDataItem();
    } else {
      Set<String> itemCodes = new HashSet<>();
      itemCodes.add(item.getItemCode());
      map = masterDataService
          .getMasterDataItems(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
              itemCodes);
      masterDataItem = map.get(item.getItemCode());
    }
    ProductAndItemSolr productAndItemSolr = productAndItemSolrRepository.findOne(item.getItemSku(),
      item.getMerchantCode());
    if (StringUtils.isNotBlank(productAndItemSolr.getMasterCatalog())) {
      String categoryCode = productAndItemSolr.getMasterCatalog().split(Constants.DELIMETER)[1];
      CategoryNamesResponse categoryNamesResponse =
          productCategoryBaseOutbound.getCategoryNames(Arrays.asList(categoryCode));
      if (Objects.nonNull(categoryNamesResponse)) {
        item.getPristineDataItem().setPristineCategory(categoryNamesResponse.getCategoryMap().get(categoryCode));
      }
    }
    item.getPristineDataItem().setPristineBrand(productAndItemSolr.getBrand());
    item.getPristineDataItem().setPristineProductName(productAndItemSolr.getItemName());
    item.getPristineDataItem()
        .setPristineListingAttributes(constructListingAttributesFromMasterDataItem(masterDataItem));
    return item;
  }

  private Map<String, String> constructListingAttributesFromMasterDataItem(MasterDataItem masterDataItem) {
    Map<String, String> pristineListingAttributes = new HashMap<>();
    if (!CollectionUtils.isEmpty(masterDataItem.getMasterDataItemAttributeValues())) {
      for (MasterDataItemAttributeValue masterDataItemAttributeValue : masterDataItem
          .getMasterDataItemAttributeValues()) {
        if (isDefiningOrVariantCreationTrue(masterDataItemAttributeValue)) {
          pristineListingAttributes.put(masterDataItemAttributeValue.getMasterDataAttribute().getAttributeName(),
              masterDataItemAttributeValue.getAttributeValue());
        }
      }
    }
    return pristineListingAttributes;
  }


  @Override
  public ProductAndItemsVO constructProductAndItemWithMasterData(String storeId, String username,
      String requestId, Product product, List<Item> items, boolean inAllProducts) throws Exception {
    MasterDataProductAndItemsVO masterDataProductAndItems = null;
    Set<String> productCodes = new HashSet<String>();
    if (StringUtils.isNotBlank(product.getProductCode())) {
      productCodes.add(product.getProductCode());
      masterDataProductAndItems = this.masterDataService
          .getMasterDataProductDetailResponseWithoutCache(storeId, username, requestId, productCodes, inAllProducts)
          .get(product.getProductCode());
    }
    return constructProductWithMasterDataAndSpecialField(product, items, masterDataProductAndItems, false);
  }

  @Override
  public Map<String, ProductMasterDataResponse> fetchMasterDataMapForTransaction(String storeId,
    List<String> itemCodeList) {
    Map<String, ProductMasterDataResponse> productMasterDataResponseMap = new HashMap<>();
    if (CollectionUtils.isEmpty(itemCodeList)) {
      return productMasterDataResponseMap;
    }
    if (itemCodeList.size() == 1) {
      try {
        productMasterDataResponseMap.put(itemCodeList.get(0),
          masterDataCacheService.getProductMasterDataForTransaction(itemCodeList.get(0)));
      } catch (Exception e) {
        log.error("Error fetching data from PCB for transaction, item code : {}, error - ",
          itemCodeList, e);
      }
    } else {
      productMasterDataResponseMap =
        fetchMasterDataMapForTransactionConcurrent(storeId, itemCodeList);
    }
    return productMasterDataResponseMap;
  }

  private Map<String, ProductMasterDataResponse> fetchMasterDataMapForTransactionConcurrent(String storeId,
    List<String> itemCodeList) {
    int batchSize = Integer.parseInt(this.systemParameterService.findValueByStoreIdAndVariable(storeId,
      SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE).getValue());
    List<List<String>> itemCodeBatches = Lists.partition(itemCodeList, batchSize);
    Map<String, ProductMasterDataResponse> responseMap = new HashMap<>();
    for (List<String> itemCodeBatch : itemCodeBatches) {
      List<Callable<ProductMasterDataResponse>> callableList = new ArrayList<>();
      for (String itemCode : itemCodeBatch) {
        callableList.add(() -> this.masterDataCacheService.getProductMasterDataForTransaction(itemCode));
      }
      List<Future<ProductMasterDataResponse>> results;
      try {
        results = executorService.invokeAll(callableList);
      } catch (Exception e) {
        log.error("Error on invoke all for list of input : {}, error - ", itemCodeBatch, e);
        continue;
      }
      for (Future<ProductMasterDataResponse> result : results) {
        ProductMasterDataResponse response;
        try {
          response = result.get();
        } catch (Exception e) {
          log.error("Error on fetch of master data from PCB. error - ", e);
          continue;
        }
        responseMap.put(
          response.getProductItemResponses().stream().findFirst().orElse(new ProductItemResponse()).getSkuCode(),
          response);
      }
    }
    return responseMap;
  }
}
